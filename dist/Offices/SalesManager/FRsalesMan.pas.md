<!-- tabs:start -->

#### **Documentation**

# Documentação do Código `FRsalesMan`

## 1. Visão Geral:

* **Objetivo Principal:**  
  O objetivo principal deste código é criar uma interface gráfica para gerenciar informações de vendedores (Sales Managers). Ele permite que os usuários visualizem, editem e interajam com os dados relacionados a vendedores, como nome, e-mail, escritório e login. Além disso, o código utiliza serviços externos para buscar e validar informações relacionadas ao escritório.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a interface gráfica e lógica do sistema.
  - **Componentes Visuais:** `TsDBEdit`, `TsLabel`, `TsPanel`, `TsBitBtn`, entre outros, para criar a interface do usuário.
  - **Serviços SOAP:** Utilizados para comunicação com serviços externos, como `TSalesManServiceUtils` e `TOfficeServiceUtils`.

* **Forma do Componente:**  
  Este código implementa um **formulário** com os seguintes elementos:
  - **Elementos do Formulário:**
    - `EDTsalesMan` (Campo de texto para código do vendedor).
    - `EDTname` (Campo de texto para o nome do vendedor).
    - `FRAMEfindOffice` (Componente para buscar informações do escritório).
    - `EDTemail` (Campo de texto para o e-mail do vendedor).
    - `EDTlogin` (Campo de texto para o login do vendedor).
  - **Ações do Formulário:**
    - Botões de ação como `BTNadd` (Adicionar) e `BTNapply` (Aplicar), que permitem adicionar ou salvar informações.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo vendedor.
  - Editar informações de um vendedor existente.
  - Buscar informações de escritórios relacionados ao vendedor.
  - Salvar alterações realizadas.

* **Componentes Principais:**
  - `EDTsalesMan`: Campo para inserir ou exibir o código do vendedor.
  - `EDTname`: Campo para inserir ou exibir o nome do vendedor.
  - `FRAMEfindOffice`: Componente para buscar e selecionar escritórios.
  - `EDTemail`: Campo para inserir ou exibir o e-mail do vendedor.
  - `EDTlogin`: Campo para inserir ou exibir o login do vendedor.
  - `FRAMEstatusInfo1`: Exibe informações de status relacionadas ao vendedor.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar":  
    `se botão adicionar for clicado então criar novo registro`.
  - Evento `OnClick` do botão "Aplicar":  
    `se botão aplicar for clicado então salvar alterações`.
  - Evento de inicialização:  
    `ao inicializar, configurar propriedades do formulário e carregar dados`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. O formulário é inicializado com o método `Create`.
  2. Propriedades do formulário, como `MasterSource`, `DataPacketName` e `ProviderService`, são configuradas.
  3. O método `m_SetFindOffice` é chamado para configurar o componente de busca de escritórios.
  4. O usuário interage com os campos e botões para adicionar ou editar informações.
  5. As ações são processadas e os dados são enviados para os serviços SOAP.

* **Dados Necessários:**
  - Código do vendedor.
  - Nome do vendedor.
  - Escritório associado.
  - E-mail do vendedor.
  - Login do vendedor.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Aplicar" só deve ser habilitado se todos os campos obrigatórios forem preenchidos corretamente.
  - O botão "Adicionar" cria um novo registro vazio para edição.

* **Filtros Disponíveis:**
  - Filtro para buscar escritórios baseado no código ou descrição.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.
  - "Formato de e-mail inválido" se o e-mail não estiver no formato correto.

* **Valores Padrão dos Campos:**
  - Nenhum valor padrão é explicitamente definido no código.

* **Validações e Condições dos Campos:**
  - `EDTemail`: Deve ser validado para garantir que o formato do e-mail seja correto.
  - `EDTsalesMan`, `EDTname`, `EDTlogin`: Devem ser preenchidos antes de salvar.

---

## 5. Funções Principais:

* **`Create` (Construtor):**  
  Configura as propriedades do formulário, inicializa o serviço de dados e configura o componente de busca de escritórios.

* **`m_SetFindOffice`:**  
  Configura o componente `FRAMEfindOffice` para buscar escritórios com base no código e descrição.

---

## 6. Consumo de Serviços API:

* **Serviço 1:**
  - **Nome do Serviço:** `TSalesManServiceUtils`.
  - **Finalidade:** Gerenciar dados de vendedores.
  - **Tipo de Chamada:** SOAP.
  - **Dados Enviados:** Não especificado no código.
  - **Dados Recebidos:** Não especificado no código.

* **Serviço 2:**
  - **Nome do Serviço:** `TOfficeServiceUtils`.
  - **Finalidade:** Buscar informações de escritórios.
  - **Tipo de Chamada:** SOAP.
  - **Dados Enviados:** Código ou descrição do escritório.
  - **Dados Recebidos:** Informações do escritório.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneFRCtrlEditSOA`, `kneFRFindEditSOA`: Utilizadas para criar e gerenciar componentes visuais.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEFindEditSOA`: Componente para busca de dados.
  - `TFRAMEstatusInfo`: Componente para exibir informações de status.

---

## 9. Listagem de Campos e Validações:

* **Campos do Formulário:**
  - `EDTsalesMan` (tipo: string, obrigatório, não definido no código).
  - `EDTname` (tipo: string, obrigatório, não definido no código).
  - `FRAMEfindOffice` (tipo: componente de busca, obrigatório).
  - `EDTemail` (tipo: string, obrigatório, formato de e-mail válido).
  - `EDTlogin` (tipo: string, obrigatório, não definido no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `officeCode` e `officeDesc` são mapeados para os campos do componente `FRAMEfindOffice`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  var
    SalesManFrame: TFRAMEsalesMan;
  begin
    SalesManFrame := TFRAMEsalesMan.Create(Self);
    SalesManFrame.Show;
  end;
  ```

* **HTML Renderizado do Formulário:**
  ```html
  <div style="width: 781px;">
    <label style="color: #4D4D4D; font-family: Tahoma;">Sales Manager:</label>
    <input type="text" placeholder="Código do Vendedor">
    <label style="color: #4D4D4D; font-family: Tahoma;">Name:</label>
    <input type="text" placeholder="Nome do Vendedor">
    <label style="color: #4D4D4D; font-family: Tahoma;">Office:</label>
    <input type="text" placeholder="Escritório">
    <label style="color: #4D4D4D; font-family: Tahoma;">Email:</label>
    <input type="email" placeholder="E-mail">
    <label style="color: #4D4D4D; font-family: Tahoma;">Login:</label>
    <input type="text" placeholder="Login">
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* O método `m_SetFindOffice` é essencial para configurar o componente de busca de escritórios.
* O construtor `Create` configura as propriedades principais do formulário e inicializa os serviços.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar informações de vendedores, com integração a serviços SOAP para buscar dados externos. No entanto, faltam validações explícitas e mensagens de erro detalhadas. A interface é bem estruturada, mas poderia ser aprimorada com mais validações e feedback ao usuário.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar vendedores, permitindo adicionar, editar e buscar informações relacionadas. Ele utiliza serviços SOAP para integração com dados externos e possui uma interface bem estruturada com componentes reutilizáveis.#### **FRsalesMan.pas**

```
unit FRsalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, kneFRStatusInfo,
  kneFRFindEditSOA, StdCtrls, Mask, DBCtrls, sDBEdit, sFrameAdapter,
  ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, Buttons,
  sBitBtn, sPanel, sLabel;

type
  TFRAMEsalesMan = class(TFRAMEBaseCtrlEditSOA)
    EDTsalesMan: TsDBEdit;
    EDTname: TsDBEdit;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    EDTemail: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLsalesman: TsLabel;
    LBLname: TsLabel;
    LBLoffice: TsLabel;
    LBLemail: TsLabel;
    LBL1: TsLabel;
    EDTlogin: TsDBEdit;
  private
    procedure m_SetFindOffice;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
  end;

var
  FRAMEsalesMan: TFRAMEsalesMan;

implementation

uses
  kneUtils, kneTypes,
  SalesManServiceUtils, OfficeServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesMan.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'salesMan';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TSalesManServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindOffice;

  // Atribui��o dos eventos de BeforeFind

  // Atribui��o do evento do After Apply

  // Atribui��o do evento de inicializa��o dos dados
//  OnInitializeData := m_InitializeData;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
end;

procedure TFRAMEsalesMan.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'officeCode';
    EditSettings.FieldNameForDesc := 'officeDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'officeCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('descrip');

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TOfficeServiceUtils.Create(FindDialog);
  end;
end;

end.
```

#### **FRsalesMan.dfm**

```
inherited FRAMEsalesMan: TFRAMEsalesMan
  Width = 781
  object LBLsalesman: TsLabel [0]
    Left = 16
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Sales Manager:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLname: TsLabel [1]
    Left = 16
    Top = 48
    Width = 31
    Height = 13
    Caption = 'Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLoffice: TsLabel [2]
    Left = 16
    Top = 80
    Width = 33
    Height = 13
    Caption = 'Office:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLemail: TsLabel [3]
    Left = 16
    Top = 112
    Width = 28
    Height = 13
    Caption = 'Email:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBL1: TsLabel [4]
    Left = 496
    Top = 16
    Width = 29
    Height = 13
    Caption = 'Login:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 781
    TabOrder = 6
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            42020000424D4202000000000000420000002800000010000000100000000100
            1000030000000002000000000000000000000000000000000000007C0000E003
            00001F0000001F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C
            1F7C1F7C1F7C1F7C1F7C1F7C1F7C731DEF10CC18CC18CC00CC181F7C1F7C1F7C
            1F7C1F7C1F7C1F7C1F7C9919CC30C64CC664C664C664C664C630C618CC181F7C
            1F7C1F7C1F7C1F7C9919CC4CC664667166716671667166716671C664C6306708
            1F7C1F7C1F7C1F7CCC4C667166716671D47AFF7FFF7F6C7E66716671C664C630
            CC181F7C1F7CD3306671667166716671737EFF7FFF7F737E667166716671C664
            C6181F7C1F7C8C4D6671667166716671D47AFF7FFF7FD47A6671667166716671
            C64CCC181F7C667166716C7E8C7D6C7ED47AFF7FFF7FD47A6C7E8C7D6C7E6671
            C664CC001F7C66716671FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F6671
            6671CC181F7C66716671FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F6671
            C664EF101F7C6671867DD47AD47AD47AB97BFF7FFF7FB97BD47AD47AD47A6671
            6671EF101F7C8C6566718C7D8C7D6C66D47AFF7FFF7FD47A867D866566716671
            C64C731D1F7C993166716C7E6C7E6C7E5673FF7FFF7FD47A867D867D867D6671
            CC301F7C1F7C1F7C8C4D66716C7E737E397FFF7FFF7FD47A6C7E8C7D6671CC4C
            99191F7C1F7C1F7C1F7C8C4D66716C7ED47A5673D47A6C7E8C656671CC4C9919
            1F7C1F7C1F7C1F7C1F7C1F7C99318C656671867D8C7D66718C65933199191F7C
            1F7C1F7C1F7C}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            42020000424D4202000000000000420000002800000010000000100000000100
            1000030000000002000000000000000000000000000000000000007C0000E003
```
<!-- tabs:end -->

