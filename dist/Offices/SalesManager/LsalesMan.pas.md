<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa uma interface para gerenciar uma lista de vendedores (Sales Managers). Ele permite que os usuários visualizem, filtrem e interajam com os dados de vendedores, incluindo a possibilidade de realizar buscas avançadas, criar, modificar e visualizar registros. O objetivo principal é fornecer uma interface amigável para gerenciar informações de vendedores de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL) para desenvolvimento da interface gráfica e lógica de negócios.
  - Componentes de terceiros como `TsLabel`, `TsEdit`, `TsCheckBox` e `TFRAMEFindEditSOA` para estilização e funcionalidades adicionais.
  - Integração com serviços externos, como `SalesManServiceUtils` e `OfficeServiceUtils`.

* **Tipo de Formulário:**
  - **Formulário de Lista (Grid Display):**
    - **Colunas do Grid:**
      - `stat` (Status) - Tipo: String.
      - `salesman` (Vendedor) - Tipo: String.
      - `name` (Nome) - Tipo: String.
      - `login` (Login) - Tipo: String.
      - `officeCode` (Código do Escritório) - Tipo: String.
      - `officeDesc` (Descrição do Escritório) - Tipo: String.
      - `email` (E-mail) - Tipo: String.
      - `lastUpd` (Última Atualização) - Tipo: Data/Hora.
      - `updBy` (Atualizado Por) - Tipo: String.
    - **Ações do Grid:**
      - Criar novo registro.
      - Modificar registro existente.
      - Visualizar detalhes de um registro.
      - Realizar busca avançada.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Criar, modificar e visualizar registros de vendedores.
  - Filtrar registros por nome, escritório e status (ativos/inativos).
  - Realizar buscas avançadas utilizando critérios específicos.

* **Componentes Principais:**
  - `EDTname`: Campo de entrada para o nome do vendedor.
  - `CHKactiveOnly`: Checkbox para filtrar apenas vendedores ativos.
  - `FRAMEfindOffice`: Componente para busca de escritórios.
  - `GridSettings`: Configuração do grid para exibição de dados.

* **Pseudo-código de Ações e Eventos:**
  - `OnClick` do botão "Novo": `se botão clicado então abrir formulário de criação`.
  - `OnClick` do botão "Modificar": `se botão clicado e registro selecionado então abrir formulário de edição`.
  - `OnClick` do botão "Visualizar": `se botão clicado e registro selecionado então abrir formulário de visualização`.
  - `OnChange` do campo "Nome": `se valor alterado então atualizar filtro`.
  - `OnCheck` do checkbox "Ativo Apenas": `se estado alterado então atualizar filtro`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`FormCreate`): Configurações iniciais e carregamento de dados.
  2. Configuração do grid (`GridSetup`): Define colunas, ordem e editores personalizados.
  3. Configuração de eventos (`EventSetup`): Associa eventos aos componentes.
  4. Interação do usuário: Ações como busca, criação, modificação e visualização de registros.

* **Dados Necessários:**
  - Nome do vendedor (opcional).
  - Escritório (opcional).
  - Status (ativo/inativo).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Novo": Disponível sempre.
  - Botão "Modificar" e "Visualizar": Disponíveis apenas se um registro estiver selecionado.
  - Filtro "Ativo Apenas": Aplicado automaticamente ao carregar os dados.

* **Filtros Disponíveis:**
  - Nome do vendedor.
  - Escritório.
  - Status (ativos/inativos).

* **Mensagens de Erro:**
  - "Nenhum registro selecionado" se tentar modificar ou visualizar sem selecionar um registro.
  - "Erro ao carregar dados" se houver falha na comunicação com o serviço.

* **Valores Padrão dos Campos:**
  - Checkbox "Ativo Apenas": Marcado por padrão.

* **Validações e Condições dos Campos:**
  - Campo "Nome": Aceita texto livre.
  - Campo "Escritório": Deve ser selecionado de uma lista.

---

## 5. Funções Principais:

* **`CreateListForm`:** Cria e inicializa o formulário de lista.
* **`GridSetup`:** Configura o grid, incluindo colunas e editores personalizados.
* **`EventSetup`:** Configura os eventos associados aos componentes.
* **`Initialize`:** Inicializa o formulário com parâmetros padrão e configura o serviço de dados.

---

## 6. Consumo de Serviços de API:

* **Serviço:** `SalesManServiceUtils`.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Parâmetros de busca e filtros.
  - **Dados Recebidos:** Lista de vendedores.
  - **Propósito:** Carregar e gerenciar dados de vendedores.
  - **Tratamento de Erros:** Exibe mensagem de erro em caso de falha.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "Escritório" é exibido sempre, mas sua funcionalidade depende da interação com o componente `FRAMEfindOffice`.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsLabel`, `TsEdit`, `TsCheckBox`: Componentes visuais estilizados.
  - `TFRAMEFindEditSOA`: Componente para busca avançada.
* **Componentes Customizados:**
  - `kneCBListSOA`: Base para o formulário de lista.
  - `SalesManServiceUtils`: Serviço para manipulação de dados de vendedores.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Nome (tipo: string, opcional).
  - Escritório (tipo: string, opcional).
  - Ativo Apenas (tipo: boolean, padrão: true).
* **Mapeamento de Valores:**
  - `stat` → Status.
  - `salesman` → Vendedor.
  - `name` → Nome.
  - `officeCode` → Código do Escritório.
  - `officeDesc` → Descrição do Escritório.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Form: TFORMLsalesMan;
  begin
    Form := TFORMLsalesMan.Create(nil);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 600px; padding: 10px; font-family: Tahoma;">
    <label for="name">Name:</label>
    <input type="text" id="name" style="width: 100%; margin-bottom: 10px;">
    <label for="office">Office:</label>
    <input type="text" id="office" style="width: 100%; margin-bottom: 10px;">
    <label>
      <input type="checkbox" checked> Active Only
    </label>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração do grid (`GridSetup`) define colunas e editores personalizados.
* Inicialização do formulário (`Initialize`) configura o serviço de dados e parâmetros padrão.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar vendedores, com suporte a filtros e integração com serviços externos. No entanto, a falta de detalhes sobre endpoints e tratamento de erros pode limitar sua funcionalidade em cenários mais complexos.

---

## 13. Resumo Curto:

Interface para gerenciar vendedores, com filtros por nome, escritório e status. Suporte a criação, modificação e visualização de registros, além de integração com serviços externos para manipulação de dados.#### **LsalesMan.pas**

```
unit LsalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, sScrollBox, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, kneEnterAsTab, kneFRGridManager, Buttons,
  sSpeedButton, ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter,
  sCheckBox, sEdit, kneCBList, kneFRFindEditSOA;

type
  TFORMLsalesMan = class(TFORMkneCBListSOA)
    LBLname: TsLabel;
    EDTname: TsEdit;
    CHKactiveOnly: TsCheckBox;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    LBLoffice: TsLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure m_SetFindOffice;
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;
    procedure EditorClosed(Sender: TObject); override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;
  end;

var
  FORMLsalesMan: TFORMLsalesMan;

implementation

uses
  kneUtils,
  MsalesMan, 
  SalesManServiceUtils,
  OfficeServiceUtils;

{$R *.dfm}


class function TFORMLsalesMan.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLsalesMan.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLsalesMan.EventSetup;
begin
  inherited;

end;

procedure TFORMLsalesMan.GridSetup;
begin
  inherited;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('stat;salesman;name;login;officeCode;officeDesc;email;lastUpd;updBy;');
    // Custom Editors ..........................................................
    AddCustomField('stat','cxEDTstatus');
  end;
end;

class procedure TFORMLsalesMan.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

  TFORMkneCBListSOA(pv_FormList).ProviderService := TSalesManServiceUtils.Create(pv_FormList);
  TFORMkneCBListSOA(pv_FormList).AutoLoad        := True;
  TFORMkneCBListSOA(pv_FormList).ServiceParams.ShowInactives := False;
end;

```

#### **LsalesMan.dfm**

```
inherited FORMLsalesMan: TFORMLsalesMan
  Left = 470
  Top = 203
  Caption = 'Sales Manager List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 153
  end
  inherited PNLsearchArea: TsPanel
    Height = 109
    inherited PNLsearchButtons: TsPanel
      Height = 107
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      Height = 107
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Height = 103
        object LBLname: TsLabel
          Left = 16
          Top = 21
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
        object LBLoffice: TsLabel
          Left = 16
          Top = 50
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
        object CHKactiveOnly: TsCheckBox
          Left = 63
          Top = 74
          Width = 80
          Height = 19
          Caption = 'Active Only'
          Checked = True
          State = cbChecked
          TabOrder = 0
          SkinData.SkinSection = 'CHECKBOX'
          ImgChecked = 0
          ImgUnchecked = 0
        end
        object EDTname: TsEdit
          Left = 64
          Top = 13
          Width = 497
          Height = 21
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'MS Sans Serif'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
        end
        inline FRAMEfindOffice: TFRAMEFindEditSOA
          Left = 64
          Top = 44
          Width = 497
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
          TabOrder = 2
          inherited PNLdesc: TPanel
            Width = 391
```
<!-- tabs:end -->

