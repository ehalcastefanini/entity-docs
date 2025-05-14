<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um formulário para gerenciar informações de mercados consignados, permitindo a edição e visualização de dados como código do mercado, descrição e região associada. Ele também integra funcionalidades para seleção de regiões através de um diálogo de busca. O objetivo é facilitar a manipulação e consulta de dados relacionados a mercados consignados.

* **Tecnologias Utilizadas:**
  - Delphi (VCL - Visual Component Library).
  - Componentes personalizados como `TFRAMEBaseCtrlEditSOA`, `TFRAMEFindEditSOA` e `TFRAMEstatusInfo`.
  - Serviços SOAP para integração com back-end (`TConsigneeMarketServiceUtils` e `TRegionServiceUtils`).

* **Tipo de Formulário:**
  - **Formulário de Edição:**
    - **Elementos do Formulário e Tipos:**
      - `EDTmarketCode` (Campo de texto vinculado ao banco de dados para o código do mercado).
      - `EDTdescription` (Campo de texto vinculado ao banco de dados para a descrição do mercado).
      - `FRAMEfindRegion` (Componente de busca para seleção de região).
      - `FRAMEstatusInfo1` (Exibe informações de status, como última atualização e usuário responsável).
    - **Ações do Formulário e Efeitos:**
      - Seleção de região através de um diálogo de busca.
      - Atualização automática de campos vinculados ao banco de dados.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Permitir a edição de informações de mercado (código, descrição e região).
  - Selecionar uma região através de um diálogo de busca.
  - Exibir informações de status, como última atualização e usuário responsável.

* **Componentes Principais:**
  - `EDTmarketCode`: Campo para entrada do código do mercado.
  - `EDTdescription`: Campo para entrada da descrição do mercado.
  - `FRAMEfindRegion`: Componente para busca e seleção de regiões.
  - `FRAMEstatusInfo1`: Exibe informações de status relacionadas ao mercado.

* **Tradução para Pseudo-código:**
  - Evento `OnCreate` do formulário: `Ao inicializar o formulário, configure propriedades e serviços necessários`.
  - Método `m_SetFindRegion`: 
    ```
    Configurar diálogo de busca para seleção de região.
    Configurar campos de código e descrição vinculados ao banco de dados.
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`Create`):
     - Configura propriedades do formulário, como `MasterSource`, `DataPacketName` e `ProviderService`.
     - Configura o painel de ações e visibilidade de componentes.
     - Chama o método `m_SetFindRegion` para configurar o componente de busca de regiões.
  2. Interação do Usuário:
     - O usuário pode preencher os campos de código e descrição.
     - O usuário pode abrir o diálogo de busca para selecionar uma região.
  3. Atualização de Dados:
     - Os campos vinculados ao banco de dados são atualizados automaticamente.

* **Dados Necessários:**
  - Código do mercado.
  - Descrição do mercado.
  - Região associada (selecionada através do diálogo de busca).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A seleção de região só é possível através do componente `FRAMEfindRegion`.
  - Os campos `EDTmarketCode` e `EDTdescription` devem estar preenchidos para salvar os dados.

* **Filtros Disponíveis:**
  - Filtro para seleção de região no diálogo de busca.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.
  - "Região inválida" se a região selecionada não for válida.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `EDTmarketCode`: Deve ser preenchido em letras maiúsculas.
  - `EDTdescription`: Deve aceitar texto livre.
  - `FRAMEfindRegion`: Deve validar a seleção de uma região válida.

---

## 5. Funções Principais:

* **`Create`:**
  - Configura o formulário e inicializa os serviços necessários.
* **`m_SetFindRegion`:**
  - Configura o componente de busca de regiões, incluindo o diálogo de seleção e os campos vinculados.

---

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - **Serviço:** `TConsigneeMarketServiceUtils`.
    - **Finalidade:** Gerenciar dados de mercados consignados.
  - **Serviço:** `TRegionServiceUtils`.
    - **Finalidade:** Buscar e selecionar regiões.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo de busca de região (`FRAMEfindRegion`) é sempre visível, mas depende da configuração do diálogo de busca para funcionar corretamente.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `InvokeRegistry`, `SOAPHTTPClient`: Para integração com serviços SOAP.
* **Componentes Personalizados:**
  - `TFRAMEBaseCtrlEditSOA`, `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `marketCode` (tipo: string, obrigatório, letras maiúsculas).
  - `description` (tipo: string, obrigatório).
  - `regionCode` (tipo: string, obrigatório, selecionado via busca).
* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `marketCode` → Coluna `marketCode`.
  - `description` → Coluna `description`.
  - `regionCode` → Coluna `regionCode`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**
  ```
  [Inicialização do Formulário] --> [Configuração de Propriedades] --> [Interação do Usuário] --> [Atualização de Dados]
  ```

* **Diagrama de Sequência:**
  ```
  Usuário --> Formulário --> Serviço SOAP (TConsigneeMarketServiceUtils)
  ```

* **Código HTML Representando o Formulário:**
  ```html
  <form style="font-family: Verdana; width: 465px;">
    <label for="marketCode" style="color: #4D4D4D;">Code:</label>
    <input id="marketCode" type="text" style="text-transform: uppercase; width: 73px;" />
    <br />
    <label for="description" style="color: #4D4D4D;">Description:</label>
    <input id="description" type="text" style="width: 200px;" />
    <br />
    <label for="region" style="color: #4D4D4D;">Region:</label>
    <input id="region" type="text" style="width: 200px;" />
  </form>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração do serviço de mercado consignado:
  ```delphi
  ProviderService := TConsigneeMarketServiceUtils.Create(self);
  ```
* Configuração do diálogo de busca de região:
  ```delphi
  with FRAMEfindRegion do
  begin
    with FindDialog do
    begin
      Caption := 'Region Selection';
      ProviderService := TRegionServiceUtils.Create(FindDialog);
    end;
  end;
  ```

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar mercados consignados, com integração a serviços SOAP e componentes personalizados. Ele é eficiente para manipulação de dados, mas depende de configurações externas para validação e persistência de dados.

---

## 13. Resumo Curto:

Formulário para gerenciar mercados consignados, permitindo edição de código, descrição e seleção de região. Integra serviços SOAP para manipulação de dados e utiliza componentes personalizados para busca e exibição de informações.#### **FRconsMarket.pas**

```
unit FRconsMarket;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo;

type
  TFRAMEconsMarket = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL1: TsLabel;
    EDTmarketCode: TsDBEdit;
    sLabel1: TsLabel;
    EDTdescription: TsDBEdit;
    sLabel2: TsLabel;
    FRAMEfindRegion: TFRAMEFindEditSOA;
  private
    { Private declarations }
    procedure m_SetFindRegion;    
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEconsMarket: TFRAMEconsMarket;

implementation

uses
  ConsigneeMarketServiceUtils, RegionServiceUtils, kneTypes;

{$R *.dfm}

{ TFRAMEconsMarket }

constructor TFRAMEconsMarket.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'ConsigneeMarket';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TConsigneeMarketServiceUtils.Create(self);

  m_SetFindRegion;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEconsMarket.m_SetFindRegion;
begin
  with FRAMEfindRegion do
  begin

    with FindDialog do
    begin
      Caption := 'Region Selection';
      ProviderService := TRegionServiceUtils.Create(FindDialog);
    end;

    with FindSettings.DataSelection do
    begin
      UseTargetDataSet := False;
      FieldNameForCode := 'regionCode';
      FieldNamesForDesc.Clear;
      FieldNamesForDesc.Add('regionDesc');
    end;

    with EditSettings do
    begin
      DataSource := DStable;
      FieldNameForCode := 'regionCode';
      FieldNameForDesc := 'region';
    end;
  end;
end;

end.
```

#### **FRconsMarket.dfm**

```
inherited FRAMEconsMarket: TFRAMEconsMarket
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTmarketCode
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 69
    Height = 13
    Caption = '&Description:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object sLabel2: TsLabel [2]
    Left = 8
    Top = 65
    Width = 44
    Height = 13
    Caption = '&Region:'
    FocusControl = FRAMEfindRegion.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 465
    TabOrder = 4
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [4]
    Left = 8
    Top = 91
    Width = 457
    Height = 42
    AutoScroll = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    inherited GRPstatus: TsGroupBox
      Width = 457
      Font.Charset = ANSI_CHARSET
      Font.Name = 'Verdana'
      ParentFont = False
      inherited DBTXTlastUpd: TsDBText
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Name = 'Verdana'
      end
      inherited DBTXTupdBy: TsDBText
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Name = 'Verdana'
      end
      inherited ICBOstat: TcxDBImageComboBox
        Width = 97
      end
    end
  end
  object EDTmarketCode: TsDBEdit [5]
    Left = 80
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'marketCode'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
```
<!-- tabs:end -->

