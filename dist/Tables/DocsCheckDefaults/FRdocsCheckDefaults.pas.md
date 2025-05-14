<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é criar um componente de interface do usuário para gerenciar e configurar os valores padrão de um checklist de documentos relacionados a consignatários e mercados. Ele permite que os usuários selecionem e configurem informações de consignatários e mercados de forma eficiente, utilizando componentes visuais e interações com serviços de backend.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TsLabel`, `TFRAMEFindEditSOA`, `TsPanel`, entre outros.
  - Serviços SOAP para integração com backend (`SOAPHTTPClient`, `TCheckListDocsDefaultServiceUtils`).
  - Manipulação de banco de dados com `DB`, `DBClient`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e Tipos:**
      - `LBLconsMkt` (Label): Exibe o texto "Cons.Market".
      - `Label1` (Label): Exibe o texto "Consignee".
      - `FRAMEfindConsignee` (Componente de busca): Permite selecionar um consignatário.
      - `FRAMEfindConsigneeMarket` (Componente de busca): Permite selecionar um mercado.
    - **Ações do Formulário e Efeitos:**
      - Configuração de visibilidade e habilitação dos campos com base no modo de acesso.
      - Integração com serviços para buscar e exibir dados de consignatários e mercados.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Configurar os campos de busca para consignatários e mercados.
  - Habilitar ou desabilitar os campos com base no modo de acesso.
  - Interagir com serviços SOAP para buscar dados relacionados.

* **Componentes Principais:**
  - `TFRAMEdocsCheckDefaults`: Classe principal que gerencia o formulário.
  - `FRAMEfindConsignee` e `FRAMEfindConsigneeMarket`: Componentes de busca para consignatários e mercados.

* **Tradução para Pseudo-código:**
  - Evento `OnSetAccessMode`: `se modo de acesso for "NEW" então habilitar campos de busca`.
  - Método `m_SetFindConsMarket`: `configurar campo de busca para mercado com fonte de dados e nomes de campo`.
  - Método `m_SetFindConsignee`: `configurar campo de busca para consignatário com fonte de dados e nomes de campo`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o construtor `Create`.
  2. Configuração de propriedades como `MasterSource`, `DataPacketName`, e `ProviderService`.
  3. Configuração dos campos de busca para consignatários e mercados.
  4. Interação do usuário com os campos de busca para selecionar valores.
  5. Integração com serviços SOAP para buscar dados.

* **Dados Necessários:**
  - Código e descrição do consignatário.
  - Código e descrição do mercado.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Campos de busca só são habilitados se o modo de acesso for "NEW".

* **Filtros Disponíveis:**
  - Filtros para código e descrição de consignatários e mercados.

* **Mensagens de Erro:**
  - Não especificado no código.

* **Valores Padrão dos Campos:**
  - Não especificado no código.

* **Validações e Condições dos Campos:**
  - Validação de código e descrição para consignatários e mercados.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Inicializa o componente e configura propriedades.
  - `m_SetAccessMode`: Define a habilitação dos campos com base no modo de acesso.
  - `m_SetFindConsMarket`: Configura o campo de busca para mercados.
  - `m_SetFindConsignee`: Configura o campo de busca para consignatários.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `TCheckListDocsDefaultServiceUtils`.
  - Propósito: Gerenciar dados de checklist de documentos padrão.
  - Dados enviados e recebidos: Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `DB`, `DBClient`: Para manipulação de dados.

* **Componentes Customizados:**
  - `TFRAMEFindEditSOA`: Componente de busca customizado.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `Cons.Market` (string, obrigatório, não especificado no código).
  - `Consignee` (string, obrigatório, não especificado no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `Cons.Market`: `marketCode` e `marketDescrip`.
  - `Consignee`: `consMkt` e `description`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Frame: TFRAMEdocsCheckDefaults;
  begin
    Frame := TFRAMEdocsCheckDefaults.Create(Self);
    Frame.ShowActionPanel := True;
    Frame.ProviderService := TCheckListDocsDefaultServiceUtils.Create(Self);
  end;
  ```
* **Captura de Tela (HTML Renderizado):**
  ```html
  <div style="width: 580px; border: 1px solid #ccc; padding: 10px;">
    <label style="display: block; margin-bottom: 5px;">Cons.Market:</label>
    <input type="text" style="width: 100%; margin-bottom: 10px;" placeholder="Cons.Market">
    <label style="display: block; margin-bottom: 5px;">Consignee:</label>
    <input type="text" style="width: 100%;" placeholder="Consignee">
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades no construtor `Create`.
* Configuração de campos de busca nos métodos `m_SetFindConsMarket` e `m_SetFindConsignee`.

---

## 12. Conclusão:

O código fornece uma estrutura robusta para gerenciar e configurar valores padrão de checklist de documentos. Ele utiliza componentes visuais e serviços SOAP para facilitar a interação do usuário. No entanto, faltam validações explícitas e mensagens de erro detalhadas.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar valores padrão de checklist de documentos, com integração a serviços SOAP e componentes visuais para seleção de consignatários e mercados. Ele é parte de um sistema maior para gerenciamento de dados empresariais.#### **FRdocsCheckDefaults.pas**

```
unit FRdocsCheckDefaults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, kneFRStatusInfo, kneFRGridEditSOA, sDBEdit, sLabel,
  sFrameAdapter, sBitBtn, sPanel, DMskin, cxGraphics, cxDBEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxImageComboBox, sDBComboBox, sBevel;

type
  TFRAMEdocsCheckDefaults = class(TFRAMEBaseCtrlEditSOA)
    LBLconsMkt: TsLabel;
    Label1: TsLabel;
    FRAMEfindConsignee: TFRAMEFindEditSOA;
    FRAMEfindConsigneeMarket: TFRAMEFindEditSOA;
  private
    procedure m_SetFindConsignee;
    procedure m_SetFindConsMarket;
    procedure m_SetAccessMode(Sender: TObject; var pv_state: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEdocsCheckDefaults: TFRAMEdocsCheckDefaults;

implementation

uses
  kneUtils, kneTypes, Global, kneFGFindUtils, kneFindDialogSOA,
  //---
  CheckListDocsDefaultServiceUtils, ConsigneeMarketServiceUtils,
  kneFREditSOA;

{$R *.dfm}

{ TFRAMEdocsCheckDefaults }

constructor TFRAMEdocsCheckDefaults.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';          // n�o necessita de estar def. na frame Master
  DataPacketName := 'CheckListDocDefMaster';
  PropertyName := '';             // n�o necessita de estar def. na frame Master
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCheckListDocsDefaultServiceUtils.Create(self);


  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
//  FRAMEstatusInfo1.DataSource := DStable;

  OnSetAccessMode := m_SetAccessMode;

  m_SetFindConsignee;
  m_SetFindConsMarket;
end;


Procedure TFRAMEdocsCheckDefaults.m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
begin
//  if (not assigned(CDStable)) or (not CDStable.Active) then   //JAR 10-02-2010  Aqui N�o � necess�ria a protec��o, pois n�o acede ao datset
//    exit;

  FRAMEfindConsigneeMarket.Enable := AccessMode = 'NEW';
  FRAMEfindConsignee.Enable := AccessMode = 'NEW';
end;

procedure TFRAMEdocsCheckDefaults.m_SetFindConsMarket;
begin
  with FRAMEfindConsigneeMarket do
  begin
    EditSettings.DataSource := nil;
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'consMkt';
    EditSettings.FieldNameForDesc := 'marketDescrip';

    // configura��o do Find Dialog
    FindDialog.Caption := 'Consignee Market Selection';
    FindDialog.Options.DataSelection.FieldNameForCode := 'marketCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

```

#### **FRdocsCheckDefaults.dfm**

```
inherited FRAMEdocsCheckDefaults: TFRAMEdocsCheckDefaults
  object LBLconsMkt: TsLabel [0]
    Left = 8
    Top = 13
    Width = 65
    Height = 13
    Hint = 'Consignee Market'
    Caption = 'Cons.Market:'
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 8
    Top = 42
    Width = 54
    Height = 13
    Hint = 'Consignee'
    Caption = 'Consignee:'
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 227
    Width = 580
    TabOrder = 2
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 787
    end
  end
  inline FRAMEfindConsignee: TFRAMEFindEditSOA [3]
    Left = 85
    Top = 37
    Width = 495
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    inherited PNLdesc: TPanel
      Left = 97
      Width = 398
      DesignSize = (
        398
        21)
      inherited DBEDesc: TsDBEdit
        Width = 398
      end
      inherited EDDesc: TsEdit
        Width = 398
      end
    end
    inherited PNLcode: TPanel
      Width = 97
      DesignSize = (
        97
        21)
      inherited DBE: TsDBEdit
        Width = 76
      end
      inherited FE: TsMaskEdit
        Width = 76
      end
      inherited PNLbutton: TPanel
        Left = 76
      end
    end
  end
  inline FRAMEfindConsigneeMarket: TFRAMEFindEditSOA [4]
    Left = 85
    Top = 8
    Width = 495
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
```
<!-- tabs:end -->

